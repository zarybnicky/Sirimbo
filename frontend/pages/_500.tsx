import { ErrorPage } from 'components/ErrorPage';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => (
  <ErrorPage
    error="Chyba"
    details="Došlo k chybě při zpracovávání vašeho dotazu. Zkuste to prosím znovu a kontaktujte administrátora, pokud chyba přetrvává."
  />
);

export default Page;
