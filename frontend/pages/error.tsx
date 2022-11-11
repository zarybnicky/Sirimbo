import { Alert, Container } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";

export default function ErrorPage() {
  const router = useRouter()
  const errorCode = router.query.id;

  return <>
    <Head>
      <title>Chyba</title>
    </Head>

    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      <Alert severity="error">
        {errorCode === 'not_found' ? <>
          <b>Stránka nenalezena.</b>
          <br />
          <br />
          Pokud si myslíte, že tu něco má být a není, kontaktujte prosím administrátora.
        </> : errorCode === 'authorization' ? <>
          <b>Nedostatečná autorizace</b>
          <br />
          <br />
          Váš požadavek nemohl být vykonán, protože vám k němu chybí
          dostatečná oprávnění. Buď se musíte přihlásit, nebo do této části webu
          nemáte přístup.
        </> : errorCode === 'ban' ? <>
          <b>Ban</b>
          <br />
          <br />
          Administrátor vám zakázal přístup, obraťte se na něj (sekce Kontakt).

        </> : errorCode == 'database_connection' ? <>
          <b>Není možné připojit se k databázi.</b>
          <br />
          <br />
          Databáze je součást, bez které nemůže TkOlymp.cz fungovat. Nefunkčnost může trvat delší dobu.
          <br />
          Pokud jsi byl přihlášen, došlo k automatickému
          odhlášení. Pokud se při příchodu na hlavní stránku
          zobrazuje stále tato zpráva, nemá cenu se pokoušet
          přihlašovat.

        </> : errorCode === 'database' ? <>
          <b>Kritická chyba v dotazu na databázi.</b>
          <br />
          <br />
          Databáze je součást, bez které nemůže TkOlymp.cz fungovat. Nefunkčnost může trvat delší dobu.
        </> : errorCode === 'not_approved' ? <>
          <b>Váš účet ještě nebyl potvrzen</b>
          <br />
          <br />
          Administrátor vám zatím nepovolil přístup na stránky.
        </> : errorCode === 'script_fatal' ? <>
          <b>Vnitřní chyba skriptu</b>
          <br />
          <br />
          Nastala chyba k jejímuž popisu neexistují konkrétní informace.
          Kontaktuj prosím administrátora a popiš všechno, co jsi dělal,
          co mohlo chybu způsobit.
        </> : <>
          Chybová stránka s daným ID nebyla nalezena.

        </>}
      </Alert>
    </Container>
  </>;
}
