import { ErrorPage } from '@/ui/ErrorPage';
import { Layout } from '@/ui/Layout';

export default function InternalErrorPage() {
  return (
    <Layout>
      <ErrorPage
        error="Chyba"
        details="Došlo k chybě při zpracovávání vašeho dotazu. Zkuste to prosím znovu a kontaktujte administrátora, pokud chyba přetrvává."
      />
    </Layout>
  );
}
