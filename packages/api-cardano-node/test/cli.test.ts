/* eslint-disable quotes */

import fs from 'fs'
import { execSync } from 'child_process'
import { getTipSync, Tip } from '@src/cli'

const cardanoCli = process.env.CARDANO_CLI_CMD ? process.env.CARDANO_CLI_CMD : 'cardano-cli'

type Address = String

interface ISlotRate {
  rate: number
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
  return utxo.TxHash + '#' + utxo.TxIx.toString()
}

function createProtocolParams (outFile: string) {
  execSync(`${cardanoCli} shelley query protocol-parameters --testnet-magic 42  --out-file ${outFile}`).toString()
}

function calculateTTL (tip: Tip, slotRate: ISlotRate, limit: number): number {
  return tip.slotNo + (slotRate.rate * limit)
}

function getUTXO (address: Address): Array<UTXO> {
  const stdout = execSync(`${cardanoCli} shelley query utxo --address ${address} --testnet-magic 42`).toString()
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
  createProtocolParams(protocolFile)
  const tip = getTipSync()
  const slotRate = { rate: 1 } // I don't currently know where to get this number from
  const ttl = calculateTTL(tip, slotRate, settings.timeLimit)
  const fromUtxo = getUTXO(settings.fromAddr)[0]
  const balanceWithoutFee = calculateChange(fromUtxo, paymentAmount, 0)
  const txOutFrom = { address: settings.fromAddr, change: balanceWithoutFee }
  const txOutTo = { address: settings.toAddr, change: paymentAmount }
  const txNoFeeFile = 'tx-no-fee.raw'
  buildTransaction(fromUtxo, txOutFrom, txOutTo, ttl, 0, txNoFeeFile)
  const fee = calculateFee(txNoFeeFile, protocolFile)
  const txWithFeeFile = 'tx.raw'
  const balanceWithFee = calculateChange(fromUtxo, paymentAmount, fee)
  const txOutFromWithFee = { address: settings.fromAddr, change: balanceWithFee }
  buildTransaction(fromUtxo, txOutFromWithFee, txOutTo, ttl, fee, txWithFeeFile)
  const txSignedFile = 'tx.signed'
  signTransaction(txWithFeeFile, settings.signingKeyFile, txSignedFile)
  submitTransaction(txSignedFile)
}

function test () {
  const fromAddr = fs.readFileSync('/Users/davidsmith/tweag/cardano/cardano-graphql/app/payment.addr').toString().trim()
  const toAddr = fs.readFileSync('/Users/davidsmith/tweag/cardano/cardano-graphql/app/payment2.addr').toString().trim()
  const settings = {
    timeLimit: 300, // I can't find out how to work this out but it seems if you set it to 30 it's too low
    fromAddr,
    toAddr,
    signingKeyFile: '/app/payment.skey'
  }
  createAndSubmitTransaction(settings)
  // wait for some time because it seems if you run the test again too soon it will fail, not sure how long though!
  // execSync(`sleep {settings.timeLimit}`)
}

describe('CLI', () => {
  it('submits a raw and signed transaction to the network using CLI', async () => {
    test()
  })
})
