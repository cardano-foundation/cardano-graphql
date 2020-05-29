import { DockerComposeStack } from '../docker'

export function serviceChoices (stack: DockerComposeStack) {
  return [...stack.statefulServices.values()].map(service => {
    return { name: service.name, value: service }
  })
}

export function serviceFromName (stack: DockerComposeStack, name: string) {
  if (!stack.statefulServices.has(name)) {
    throw new Error(`Unknown service ${name}`)
  }
  return stack.statefulServices.get(name)
}

export function servicesFromNames (stack: DockerComposeStack, names: string) {
  return names.split(',').map((name: string) => serviceFromName(stack, name))
}
