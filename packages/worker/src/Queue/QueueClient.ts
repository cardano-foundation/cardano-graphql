
export interface QueueClient {
  subscribe: <Msg>(queue:string, onMessage: (msg:Msg | null) => void) => Promise<void>;

  unsubscribe: (queue:string) => Promise<void>;
}
