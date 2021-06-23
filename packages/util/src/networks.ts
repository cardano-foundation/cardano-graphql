import { Point } from '@rhyslbw/ogmios-schema'

export interface Era {
  lastPoint?: Point
}

export interface NetworkInfo {
  eras: {
    byron: Era
    shelley: Era
    allegra: Era
    mary: Era
  }
  name: string
}

export const mainnet: NetworkInfo = {
  eras: {
    byron: {
      lastPoint: {
        hash: 'f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457',
        slot: 4492799
      }
    },
    shelley: {
      lastPoint: {
        hash: '4e9bbbb67e3ae262133d94c3da5bffce7b1127fc436e7433b87668dba34c354a',
        slot: 16588737
      }
    },
    allegra: {
      lastPoint: {
        hash: '69c44ac1dda2ec74646e4223bc804d9126f719b1c245dadc2ad65e8de1b276d7',
        slot: 23068793
      }
    },
    mary: {}
  },
  name: 'mainnet'
}

export const testnet: NetworkInfo = {
  eras: {
    byron: {
      lastPoint: {
        hash: 'ad6825e001ae4e529af25be11ddb488114033c302a233bc6d6b9c4617793f022',
        slot: 1581712
      }
    },
    shelley: {
      lastPoint: {
        hash: '1a9636ff1dbb54dff1ad652f5f9e7a734efddf0f2276cc665201589358dcfd8d',
        slot: 13694314
      }
    },
    allegra: {
      lastPoint: {
        hash: '450f1646951a7e0b27dfa80e1012e4dfbab3281b2fe0fea736bb7b94e240ddc9',
        slot: 17042486
      }
    },
    mary: {}
  },
  name: 'testnet'
}

export const networkInfoFromMagic = (magic: number) => {
  switch (magic) {
    case 764824073 :
      return mainnet
    case 1097911063 :
      return testnet
  }
}
