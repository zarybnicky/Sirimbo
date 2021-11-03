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

class DateElement extends HTMLElement {
    connectedCallback() {
        const acc = this.getAttribute('acc') || '';
        const am = this.getAttribute('am') || '';
        const msg = this.getAttribute('msg') || '';
        const ss = this.getAttribute('ss') || '';
        const vs = this.getAttribute('vs') || '';
        const ks = this.getAttribute('ks') || '';

        const spayd = mkSpayd(acc, am, msg, ss, vs, ks);
        ReactDOM.render(<QRCode value={spayd} level="M" size={256} />, this);
    }
}
customElements.define('qr-payment', DateElement);
