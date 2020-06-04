import os, { CpuInfo } from 'os'

export interface SystemInfo {
  os: {
    arch: ReturnType<typeof os.arch>,
    cpus: {
      count: number,
      model: CpuInfo['model'],
    }
    release: ReturnType<typeof os.release>
    totalMem: ReturnType<typeof os.totalmem>
    type: ReturnType<typeof os.type>
  }
  version: string,
}

export function getSystemInfo (version: string): SystemInfo {
  return {
    os: {
      arch: os.arch(),
      cpus: {
        count: os.cpus().length,
        model: os.cpus()[0].model
      },
      release: os.release(),
      totalMem: os.totalmem(),
      type: os.type()
    },
    version
  }
}
