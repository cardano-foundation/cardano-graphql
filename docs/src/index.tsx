import React from 'react';
import ReactDOM from 'react-dom';
import fetch from 'cross-fetch';
import { Voyager } from 'graphql-voyager';
import { getIntrospectionQuery } from 'graphql';

function introspectionProvider() {
  return fetch('https://cardano-graphql-mainnet.daedalus-operations.com/', {
    method: 'post',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({query: getIntrospectionQuery()}),
  }).then(response => response.json());
}

ReactDOM.render(<Voyager
  introspection={introspectionProvider}
  workerURI={process.env.PUBLIC_URL + '/voyager.worker.js'}
/>, document.getElementById('root'));
