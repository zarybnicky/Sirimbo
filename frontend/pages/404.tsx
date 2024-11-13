import { ErrorPage } from '@/ui/ErrorPage';
import { Layout } from '@/components/layout/Layout';

export default function NotFoundErrorPage() {
  return (
    <Layout>
      <ErrorPage
        error="Stránka nenalezena"
        details="Pokud si myslíte, že tu něco chybí, kontaktujte prosím administrátora"
      />
    </Layout>
  );
}
