import { TitleBar } from '@/ui/TitleBar';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';

export default function ContactPage() {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <TitleBar title="Kontakt" />
      <div className="prose prose-accent">
        <h2>Fakturační údaje:</h2>
        <p>
          Taneční klub Olymp Olomouc, z. s.
          <br />
          Jiráskova 381/25, 779 00, Olomouc - Hodolany
          <br />
          IČO: 68347286
          <br />
          Oddíl L, vložka 4133, Krajský soud v Ostravě, pobočka v Olomouci.
          <br />
          Datová schránka: g2q66be
          <br />
          Číslo účtu: 1806875329/0800
        </p>
      </div>

      <div className="col-popout gap-4 my-8 grid lg:grid-cols-2">
        <div className="lg:col-span-2 prose prose-accent">
          <h2>Kontaktní osoby:</h2>
        </div>

        <div className="prose prose-accent">
          <h3>Statutární zástupce</h3>
          <p>
            <b>Mgr. Miroslav Hýža</b>
            <br />
            Předseda, šéftrenér
            <br />
            Tel.: 737 545 525
            <br />
            E-mail: miroslav.hyza@tkolymp.cz
          </p>

          <h3>Tajemník</h3>
          <p>
            <b>Ing. Aleš Pala</b>
            <br />
            Fakturace, platby členských příspěvků, potvrzení o platbách
            <br />
            E-mail: ales.pala@tkolymp.cz
          </p>

          <h3>Agendy ČSTS</h3>
          <p>
            <b>Mgr. Marie Hýžová ml.</b>
            <br />
            Přihlášky do ČSTS, zdravotní prohlídky, aj.
            <br />
            E-mail: marie.hyzova@tkolymp.cz
          </p>

          <h3>Klubové oblečení a doplňky</h3>
          <p>
            <b>Bc. Hana Anna Šišková</b>
            <br />
            Nákup triček, teplákových souprav a doplňků
            <br />
            Tel.: 737 074 566
            <br />
            E-mail: hanaanna.siskova@tkolymp.cz
          </p>
        </div>

        <div className="prose prose-accent">
          <h3>Taneční kroužky Olymp Dance</h3>
          <p>
            <b>Martin Matýsek</b>
            <br />
            Vedoucí projektu Olymp Dance
            <br />
            Tel.: 774 090 200
            <br />
            E-mail: info@olympdance.cz
          </p>

          <h3>Dětské skupiny na ZŠ Holečkova</h3>
          <p>
            <b>Mgr. Marie Hýžová</b>
            <br />
            Místopředsedkyně z. s.
            <br />
            Tel.: 604 756 085
            <br />
            E-mail: marie.hy@seznam.cz
          </p>

          <h3>Pro-Am sekce</h3>
          <p>
            <b>Ing. Roman Pecha</b>
            <br />
            Tel.: 737 477 599
            <br />
            E-mail: roman.pecha@tkolymp.cz
          </p>
        </div>
      </div>
    </Layout>
  );
}
