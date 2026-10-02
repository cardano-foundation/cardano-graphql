import {
  ASTNode,
  DocumentNode,
  GraphQLInputType,
  GraphQLSchema,
  Kind,
  NamedTypeNode,
  TypeInfo,
  TypeNode,
  getNamedType,
  isInputObjectType,
  isListType,
  isNonNullType,
  visit,
  visitWithTypeInfo
} from 'graphql'

const BYTEA = 'bytea'
const HEX_PREFIX = '\\x'

interface DelegatedRequest {
  document: DocumentNode
  variables: Record<string, any>
}

const withHexPrefix = (value: string) =>
  value.startsWith(HEX_PREFIX) ? value : `${HEX_PREFIX}${value}`

const toTargetValue = (value: any, type: GraphQLInputType): any => {
  if (value === null || value === undefined) return value
  if (isNonNullType(type)) return toTargetValue(value, type.ofType)
  if (isListType(type)) {
    return Array.isArray(value)
      ? value.map(item => toTargetValue(item, type.ofType))
      : toTargetValue(value, type.ofType)
  }
  if (isInputObjectType(type)) {
    const fields = type.getFields()
    return Object.keys(value).reduce((result: Record<string, any>, key) => {
      result[key] = fields[key] ? toTargetValue(value[key], fields[key].type) : value[key]
      return result
    }, {})
  }
  if (type.name === BYTEA && typeof value === 'string') return withHexPrefix(value)
  return value
}

const namedTypeNode = (node: TypeNode): NamedTypeNode =>
  node.kind === Kind.NAMED_TYPE ? node : namedTypeNode(node.type)

const renameNamedType = (node: TypeNode, name: string): TypeNode =>
  node.kind === Kind.NAMED_TYPE
    ? { ...node, name: { ...node.name, value: name } }
    : { ...node, type: renameNamedType(node.type, name) } as TypeNode

const isVariableDefinition = (parent: ASTNode | ReadonlyArray<ASTNode>) =>
  !Array.isArray(parent) && (parent as ASTNode).kind === Kind.VARIABLE_DEFINITION

export class NestedArgumentsTransform {
  public transformRequest (
    request: DelegatedRequest,
    delegationContext?: Record<string, any>
  ): DelegatedRequest {
    const targetSchema: GraphQLSchema | undefined = delegationContext?.targetSchema
    if (!targetSchema) return request
    const typeInfo = new TypeInfo(targetSchema)
    const variableTypes = new Map<string, GraphQLInputType>()
    const document = visit(request.document, visitWithTypeInfo(typeInfo, {
      [Kind.VARIABLE]: (node, _key, parent) => {
        if (isVariableDefinition(parent)) return
        const inputType = typeInfo.getInputType()
        if (inputType && !variableTypes.has(node.name.value)) {
          variableTypes.set(node.name.value, inputType)
        }
      },
      [Kind.STRING]: (node) => {
        const inputType = typeInfo.getInputType()
        if (inputType && getNamedType(inputType).name === BYTEA && !node.value.startsWith(HEX_PREFIX)) {
          return { ...node, value: withHexPrefix(node.value) }
        }
      }
    }))
    const variables = { ...request.variables }
    const documentWithTargetVariables = visit(document, {
      [Kind.VARIABLE_DEFINITION]: (node) => {
        const name = node.variable.name.value
        const targetType = variableTypes.get(name)
        if (!targetType) return
        if (name in variables) variables[name] = toTargetValue(variables[name], targetType)
        const targetTypeName = getNamedType(targetType).name
        if (namedTypeNode(node.type).name.value === targetTypeName) return
        return { ...node, type: renameNamedType(node.type, targetTypeName) }
      }
    })
    return { ...request, document: documentWithTargetVariables, variables }
  }
}
