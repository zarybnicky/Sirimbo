import { ErrorPage } from '@app/ui/ErrorPage';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => (
  <ErrorPage
    error="Stránka nenalezena"
    details="Pokud si myslíte, že tu něco chybí, kontaktujte prosím administrátora"
  />
);

export default Page;
