import { ExhibitionRequestForm } from "components/ExhibitionRequestForm";
import { Heading } from "components/Heading";
import { Layout } from "components/layout/Layout";

export default function ExhibitionsPage() {
    return <>
        <Heading image="" color={{r: 255, g: 60, b: 60, a: 90}} text="Taneční vystoupení" />
        <div className="w-full mb-8 prose">
            <p>Hledáte taneční vystoupení na svůj ples, firemní večírek nebo jinou společenskou akci? Máme pro Vás řešení!</p>

            <ul>
                <li>Standardní a latinskoamerické tance</li>
                <li>Mistři a vicemistři ČR a finalisty mezinárodních soutěží</li>
                <li>Všechny výkonnostní úrovně</li>
                <li>Věkové kategorie 12-20 let</li>
                <li>Délka vystoupení 6-10 minut za jeden vstup</li>
                <li>Cena je již od 2000 Kč</li>
            </ul>

            <p>
                Můžete nás kontaktovat na 737 545 525 nebo nezávazně vyplnit formulář ZDE a my se Vám ozveme.
            </p>
        </div>

        video

        <ExhibitionRequestForm />
    </>;
}

ExhibitionsPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
