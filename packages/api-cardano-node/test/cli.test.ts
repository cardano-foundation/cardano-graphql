/* eslint-disable quotes */

import fs from 'fs'
import { execSync } from 'child_process'

const cardanoCli = 'docker exec -t d8b90c90e08b cardano-cli'

type Address = String

interface ISlotRate {
  rate: number
}

type Tip = {
  blockNo: number,
  headerHash: String,
  slotNo: number
}

type UTXO = {
  TxHash: String,
  TxIx: number,
  Lovelace: number
}

type TxOut = {
  address: Address,
  change: number
}

function txOutString (txOut: TxOut): String {
  return txOut.address + '+' + txOut.change.toString()
}

function txInString (utxo: UTXO): String {
  return utxo.TxHash + '#' + utxo.Lovelace
}

function createProtocolParams (outFile: string): ISlotRate {
  // const stdout = execSync(`${cardanoCli} shelley query protocol-parameters --testnet-magic 42`).toString()
  // we do this only because we are running locally against docker in dev, normally we could write the result to a file
  execSync(`${cardanoCli} shelley query protocol-parameters --testnet-magic 42  --out-file ${outFile}`).toString()
  // console.log(stdout)
  // console.log(outFile)
  // fs.writeFileSync(outFile, stdout)
  return { rate: 1 } // JSON.parse(stdout)  // supposed to be form protocolParams but it doesn't seem to have it
}

function getTip (): Tip {
  const stdout = execSync(`${cardanoCli} shelley query tip --testnet-magic 42`).toString()
  return JSON.parse(stdout)
}

function calculateTTL (tip: Tip, slotRate: ISlotRate, limit: number): number {
  return tip.slotNo + (slotRate.rate * limit)
}

function getUTXO (address: Address): Array<UTXO> {
  const stdout = execSync(`${cardanoCli} shelley query utxo --address ${address} --testnet-magic 42`).toString()
  console.log(address)
  console.log(stdout)
  const rowToUtxo = function (row: string) {
    const fields = row.trim().split(/\W+/)
    return {
      TxHash: fields[0],
      TxIx: Number.parseInt(fields[1]),
      Lovelace: Number.parseInt(fields[2])
    }
  }
  const utxos = stdout.trim().split('\n').slice(2).map(rowToUtxo)
  return utxos
}

function calculateChange (from: UTXO, amount: number, fee: number): number {
  return from.Lovelace - amount - fee
}

function buildTransaction (utxoIn: UTXO, from: TxOut, to: TxOut, ttl: number, fee: number, txOutFile: String) {
  const txIn = txInString(utxoIn)
  const txOutFrom = txOutString(from)
  const txOutTo = txOutString(to)
  execSync(`${cardanoCli} shelley transaction build-raw --tx-in ${txIn} --tx-out ${txOutFrom} --tx-out ${txOutTo} --ttl ${ttl} --fee ${fee} --out-file ${txOutFile}`).toString()
}

function calculateFee (txBodyFile: String, protocolParamsFile: String): number {
  const stdout = execSync(`${cardanoCli} shelley transaction calculate-min-fee --tx-body-file ${txBodyFile} --tx-in-count 1 --tx-out-count 2 --testnet-magic 42 --protocol-params-file ${protocolParamsFile} --witness-count 0 --byron-witness-count 0`).toString()
  return Number.parseInt(stdout.replace('Lovelace', '').trim())
}

function signTransaction (txBodyFile: String, signingKeyFile: String, txOutFile: String) {
  execSync(`${cardanoCli} shelley transaction sign --tx-body-file ${txBodyFile} --signing-key-file ${signingKeyFile} --testnet-magic 42 --out-file ${txOutFile}`).toString()
}

function submitTransaction (txBodyFile: String) {
  execSync(`${cardanoCli} shelley transaction submit --tx-file ${txBodyFile} --testnet-magic 42`).toString()
}

type Settings = {
  timeLimit: number
  fromAddr: string
  toAddr: string
  signingKeyFile: string
}

function createAndSubmitTransaction (settings: Settings) {
  const paymentAmount = 10
  const protocolFile = 'protocol.json'
  const protocolParams = createProtocolParams(protocolFile)
  const tip = getTip()
  const ttl = calculateTTL(tip, protocolParams, settings.timeLimit)
  const fromUtxo = getUTXO(settings.fromAddr)[0]
  const toUtxos = getUTXO(settings.toAddr)
  const toLovelace = toUtxos[0] ? toUtxos[0].Lovelace : 0
  const balanceWithoutFee = calculateChange(fromUtxo, paymentAmount, 0)
  const txOutFrom = { address: settings.fromAddr, change: balanceWithoutFee }
  const txOutTo = { address: settings.toAddr, change: toLovelace + paymentAmount }
  const txNoFeeFile = 'tx-no-fee.raw'
  console.log("TX From: ", txOutFrom)
  console.log("TX To: ", txOutTo)
  buildTransaction(fromUtxo, txOutFrom, txOutTo, ttl, 0, txNoFeeFile)
  const fee = calculateFee(txNoFeeFile, protocolFile)
  const txWithFeeFile = 'tx.raw'
  const balanceWithFee = calculateChange(fromUtxo, paymentAmount, fee)
  const txOutFromWithFee = { address: settings.fromAddr, change: balanceWithFee }
  console.log('fee: ', fee)
  console.log("TX From: ", txOutFromWithFee)
  buildTransaction(fromUtxo, txOutFromWithFee, txOutTo, ttl, fee, txWithFeeFile)
  const txSignedFile = 'tx.signed'
  signTransaction(txWithFeeFile, settings.signingKeyFile, txSignedFile)
  submitTransaction(txSignedFile)
}

function test () {
  const fromAddr = fs.readFileSync('/Users/davidsmith/tweag/cardano/cardano-graphql/app/payment.addr').toString().trim()
  const toAddr = fs.readFileSync('/Users/davidsmith/tweag/cardano/cardano-graphql/app/payment2.addr').toString().trim()
  const settings = {
    timeLimit: 30,
    fromAddr,
    toAddr,
    signingKeyFile: '/app/payment.skey'
  }
  createAndSubmitTransaction(settings)
  // wait for 30 seconds
  // execSync(`sleep {settings.timeLimit}`)
  const finalUtxo = getUTXO(settings.fromAddr)
  console.log(finalUtxo)
}

describe('CLI', () => {
  it('submits a raw and signed transaction to the network using CLI', async () => {
    test()
  })
})
