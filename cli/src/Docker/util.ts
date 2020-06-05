import { QuestionCollection } from 'inquirer'
import { DockerComposeStack, StatefulService } from './index'

export function selectServicesFromStack (stack: DockerComposeStack, multi = true): QuestionCollection {
  const label = multi === true ? 'services' : 'service'
  return {
    name: label,
    message: `Select ${label}`,
    type: multi === true ? 'checkbox' : 'list',
    choices: [...stack.statefulServices.values()].map(service => {
      return {
        name: service.name,
        value: service
      }
    }),
    validate (input) {
      if (input.length === 0) {
        throw new Error('At least one service must be selected')
      }
      return true
    }
  }
}

export function servicesFromNameArg (stack: DockerComposeStack, names: string): StatefulService[] {
  return names.split(',').map((name: string) => {
    if (!stack.statefulServices.has(name)) {
      throw new Error(`Unknown service ${name}`)
    }
    return stack.statefulServices.get(name)
  })
}
