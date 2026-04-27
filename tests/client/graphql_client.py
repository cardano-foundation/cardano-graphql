"""
Thin GraphQL HTTP client for the cardano-graphql test framework.

Responsibilities (deliberately narrow):
- POST {query, variables, operationName} to the configured /graphql endpoint.
- Return a lightweight Response object exposing status_code, json_body, data, errors.
- Do NOT handle retries (that's the readiness fixture's job) or caching.
- Do NOT auto-raise on GraphQL errors — some tests (negative) assert on them.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import httpx


class GraphQLError(RuntimeError):
    """Raised by .data_or_raise() when the response has GraphQL errors."""

    def __init__(self, errors: list[dict[str, Any]], query: str):
        self.errors = errors
        self.query = query
        super().__init__(f"GraphQL errors: {errors}")


@dataclass
class Response:
    status_code: int
    json_body: dict[str, Any]

    @property
    def data(self) -> dict[str, Any] | None:
        return self.json_body.get("data")

    @property
    def errors(self) -> list[dict[str, Any]]:
        return self.json_body.get("errors") or []

    def data_or_raise(self, query: str = "") -> dict[str, Any]:
        if self.errors:
            raise GraphQLError(self.errors, query)
        if self.data is None:
            raise RuntimeError(f"Response has no 'data' and no 'errors': {self.json_body}")
        return self.data


class GraphQLClient:
    def __init__(self, base_url: str, timeout_s: float = 30.0):
        self.base_url = base_url.rstrip("/")
        self._client = httpx.Client(timeout=timeout_s)

    def __enter__(self) -> "GraphQLClient":
        return self

    def __exit__(self, *_exc) -> None:
        self._client.close()

    def close(self) -> None:
        self._client.close()

    def execute(
        self,
        query: str,
        variables: dict[str, Any] | None = None,
        operation_name: str | None = None,
    ) -> Response:
        body: dict[str, Any] = {"query": query, "variables": variables or {}}
        if operation_name:
            body["operationName"] = operation_name
        resp = self._client.post(
            f"{self.base_url}/graphql",
            json=body,
            headers={"Content-Type": "application/json"},
        )
        try:
            payload = resp.json()
        except Exception:  # noqa: BLE001
            payload = {"errors": [{"message": resp.text}]}
        return Response(status_code=resp.status_code, json_body=payload)

    def raw_query(self, query: str, variables: dict[str, Any] | None = None) -> dict[str, Any]:
        """Execute and return the raw JSON body. Used by readiness/sanity paths."""
        return self.execute(query, variables=variables).json_body
