import { ErrorPage } from '@app/ui/ErrorPage';
import { Layout } from '@/components/layout/Layout';

const Page = () => (
  <Layout>
    <ErrorPage
      error="Stránka nenalezena"
      details="Pokud si myslíte, že tu něco chybí, kontaktujte prosím administrátora"
    />
  </Layout>
);

export default Page;
