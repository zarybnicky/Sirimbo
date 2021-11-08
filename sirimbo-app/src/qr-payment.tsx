import * as React from 'react';
import * as ReactDOM from 'react-dom';
import IBAN from 'iban';
import QRCode from 'qrcode.react';

export function mkSpayd(
    acc: string, am: string, msg: string, ss: string, vs: string, ks: string,
) {
    const [num, bank] = acc.split('/');
    let [pref, acct] = num.split('-');
    if (!acct) {
        acct = pref;
        pref = '';
    }
    const bban = bank.padStart(4, '0') + pref.padStart(6, '0') + acct.padStart(10, '0');
    const iban = IBAN.fromBBAN('CZ', bban);
    return [
        "SPD*1.0*CC:CZK",
        "ACC:" + iban,
        "AM:" + am,
        "MSG:" + msg,
        "X-VS:" + vs,
        "X-SS:" + ss,
        "X-KS:" + ks,
    ].join("*");
}

export class QRCodeElement extends HTMLElement {
    connectedCallback() {
        const spayd = mkSpayd(
            this.getAttribute('acc') || '',
            this.getAttribute('am') || '',
            this.getAttribute('msg') || '',
            this.getAttribute('ss') || '',
            this.getAttribute('vs') || '',
            this.getAttribute('ks') || ''
        );
        ReactDOM.render(<QRCode value={spayd} level="M" size={256} />, this);
    }
}
