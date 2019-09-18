import axios, { AxiosRequestConfig } from 'axios'

export type Config = {
  requestConfig: AxiosRequestConfig
}

export function JormungandrNodeClient (config: Config) {
  const httpClient = axios.create(config.requestConfig)
  const sendRequest = async (url: string, rc: AxiosRequestConfig) => {
    try {
      return await httpClient.request({ url, ...rc })
    } catch (error) {
      console.error (error.message)
      throw error
    }
  }
  return {
    async submitTransaction (transaction: any) {
      return await sendRequest(`message/`, {
        data: { transaction },
        method: 'post',
        headers: {
          'Content-Type': 'application/octet-stream'
        }
      })
    }
  }
}
