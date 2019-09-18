import { JormungandrNodeClient } from '../lib/JormungandrNodeClient'

export type DesktopJormungandrContext = {
  nodeClient: ReturnType<typeof JormungandrNodeClient>
}
