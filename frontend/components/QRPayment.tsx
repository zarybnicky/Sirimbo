import * as React from 'react';
import IBAN from 'iban';
import QRCode from 'qrcode.react';

export function mkSpayd({ acc, am, msg, ss, vs, ks }: QRPaymentProps) {
  const [num, bank] = acc.split('/') as [string, string];
  let [pref, acct] = num.split('-') as [string] | [string, string];
  if (!acct) {
    acct = pref;
    pref = '';
  }
  const bban = bank.padStart(4, '0') + pref.padStart(6, '0') + acct.padStart(10, '0');
  const iban = IBAN.fromBBAN('CZ', bban);
  return [
    'SPD*1.0*CC:CZK',
    'ACC:' + iban,
    'AM:' + am,
    'MSG:' + msg,
    'X-VS:' + vs,
    'X-SS:' + ss,
    'X-KS:' + ks,
  ].join('*');
}

interface QRPaymentProps {
  acc: string;
  am: string;
  msg: string;
  ss?: string;
  vs?: string;
  ks?: string;
}

export const QRPayment = (props: QRPaymentProps) => {
  const spayd = mkSpayd(props);
  return <QRCode value={spayd} level="M" size={256} />;
};
