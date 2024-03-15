import { ErrorPage } from '@/ui/ErrorPage';
import { Layout } from '@/components/layout/Layout';

const Page = () => (
  <Layout>
    <ErrorPage
      error="Chyba"
      details="Došlo k chybě při zpracovávání vašeho dotazu. Zkuste to prosím znovu a kontaktujte administrátora, pokud chyba přetrvává."
    />
  </Layout>
);

export default Page;
