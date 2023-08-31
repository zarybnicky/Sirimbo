import { TitleBar } from '@app/ui/TitleBar';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';

const Page = () => (
  <Layout hideTopMenuIfLoggedIn>
    <TitleBar title="Kontakt" />
    <div className="prose prose-accent">
      <h2>Fakturační údaje:</h2>
      <p>
        Taneční klub Olymp Olomouc, z. s.
        <br />
        Jiráskova 381/25, 779 00, Olomouc - Hodolany
        <br />
        lČO: 68347286, oddíl L. vl. 4133, Krajský soud v Ostravě, pobočka v Olomouci.
        <br />
        Datová schránka: g2q66be
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
          tel: 737 545 525
          <br />
          email: miroslav.hyza@tkolymp.cz
        </p>

        <h3>Tajemník</h3>
        <p>
          <b>Ing. Aleš Pala</b>
          <br />
          Fakturace, platby členských příspěvků, potvrzení o platbách
          <br />
          email: ales.pala@tkolymp.cz
        </p>

        <h3>Agendy ČSTS</h3>
        <p>
          <b>Mgr. Marie Hýžová ml.</b>
          <br />
          Přihlášky do ČSTS, zdravotní prohlídky, aj.
          <br />
          tel: 737 644 899
          <br />
          email: marie.hyzova@tkolymp.cz
        </p>

        <h3>Klubové oblečení a doplňky</h3>
        <p>
          <b>Bc. Hana Anna Šišková</b>
          <br />
          Nákup triček, teplákových souprav a doplňků
          <br />
          tel: 737 074 566
          <br />
          email: hanaanna.siskova@tkolymp.cz
        </p>
      </div>

      <div className="prose prose-accent">
        <h3>Taneční kroužky Olymp Dance</h3>
        <p>
          <b>Martin Matýsek</b>
          <br />
          Vedoucí projektu Olymp Dance
          <br />
          tel: 774 090 200
          <br />
          email: info@olympdance.cz
        </p>

        <h3>Dětské skupiny na ZŠ Holečkova</h3>
        <p>
          <b>Mgr. Marie Hýžová</b>
          <br />
          Místopředseda z. s.
          <br />
          tel: 604 756 085
          <br />
          email: marie.hy@seznam.cz
        </p>

        <h3>Pro-Am sekce</h3>
        <p>
          <b>Ing. Roman Pecha</b>
          <br />
          tel: 737 477 599
          <br />
          email: roman.pecha@tkolymp.cz
        </p>
      </div>
    </div>
  </Layout>
);

export default Page;
