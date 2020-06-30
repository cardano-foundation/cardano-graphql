import { Logger } from 'ts-log'
import inquirer from 'inquirer'
import BottomBar from 'inquirer/lib/ui/bottom-bar'

enum LogLevel {
  TRACE = 0,
  DEBUG = 1,
  WARN = 2,
  INFO = 3,
  ERROR = 4
}

export class CliLogger implements Logger {
  private bottomBar: BottomBar

  private minLogLevel: LogLevel

  public constructor (minLogLevel = LogLevel.INFO) {
    this.bottomBar = new inquirer.ui.BottomBar()
    this.minLogLevel = minLogLevel
  }

  public trace (message: any, optionalParams?: any[]): void {
    if (this.minLogLevel > LogLevel.TRACE) return
    const optionalJson = optionalParams ? JSON.stringify(optionalParams) : ''
    this.bottomBar.log.write(`TRACE: ${message} ${optionalJson}`)
  }

  public debug (message: any, optionalParams?: any[]): void {
    if (this.minLogLevel > LogLevel.DEBUG) return
    const optionalJson = optionalParams ? JSON.stringify(optionalParams) : ''
    this.bottomBar.log.write(`DEBUG: ${message} ${optionalJson}`)
  }

  public warn (message: any, optionalParams?: any[]): void {
    if (this.minLogLevel > LogLevel.WARN) return
    const optionalJson = optionalParams ? JSON.stringify(optionalParams) : ''
    this.bottomBar.log.write(`WARN: ${message} ${optionalJson}`)
  }

  public info (message: any, optionalParams?: any[]): void {
    if (this.minLogLevel > LogLevel.INFO) return
    const optionalJson = optionalParams ? JSON.stringify(optionalParams) : ''
    this.bottomBar.log.write(`${message} ${optionalJson}`)
  }

  public error (message: any, optionalParams?: any[]): void {
    if (this.minLogLevel > LogLevel.ERROR) return
    const optionalJson = optionalParams ? JSON.stringify(optionalParams) : ''
    this.bottomBar.log.write(`ERROR: ${message} ${optionalJson}`)
  }
}
