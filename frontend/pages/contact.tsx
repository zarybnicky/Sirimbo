import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { NextSeo } from 'next-seo';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <NextSeo title="Kontakt" />
      <Heading>Kontakt</Heading>

      <div className="prose mt-8">
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
        <div className="lg:col-span-2 prose">
          <h2>Kontaktní osoby:</h2>
        </div>

        <div className="prose">
          <h3>Statutární zástupce:</h3>
          <p>
            <b>Mgr. Miroslav Hýža</b>
            <br />
            Předseda, šéftrenér
            <br />
            tel: 737 545 525
            <br />
            email: miroslav.hyza@tkolymp.cz
          </p>

          <h3>Fakturace:</h3>
          <p>
            <b>Ing. Aleš Pala</b>
            <br />
            Ekonom
            <br />
            email: ales.pala@tkolymp.cz
          </p>

          <h3>
            Platby členských příspěvků, noví zájemci - Olomouc, potvrzení o platbách
          </h3>
          <p>
            <b>Mgr. Lucie Benýšková</b>
            <br />
            Vedoucí členských agend
            <br />
            email: lucie.benyskova@tkolymp.cz
          </p>

          <h3>Taneční kroužky OlympDance</h3>
          <p>
            <b>Martin Matýsek</b>
            <br />
            Vedoucí projektu OlympDance
            <br />
            tel: 774 090 200
            <br />
            email: info@olympdance.cz
          </p>
        </div>

        <div className="prose">
          <h3>Prostějov - noví zájemci i členové</h3>
          <p>
            <b>Roman Pecha</b>
            <br />
            Vedoucí pobočky
            <br />
            tel: 737 477 599
            <br />
            email: roman.pecha@tkolymp.cz
          </p>

          <h3>Přerov - noví zájemci i členové</h3>
          <p>
            <b>Marie Hýžová ml.</b>
            <br />
            Vedoucí pobočky
            <br />
            tel: 737 644 899
            <br />
            email: marie.hyzova@tkolymp.cz
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
            <b>Vilém Šír</b>
            <br />
            Vedoucí lektor Pra-Am
            <br />
            tel: 736 628 504
            <br />
            email: vilem.sir@tkolymp.cz
          </p>
        </div>
      </div>

      <CallToAction />
    </>
  );
}

Page.hideTopMenuIfLoggedIn = true;

export default Page;