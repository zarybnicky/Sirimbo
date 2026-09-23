import { getRequestContext } from '@/lib/server/tenant';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import { RegistrationForm } from './RegistrationForm';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Přihláška nového člena',
};

export default async function RegisterPage() {
  const { tenant, claims } = await getRequestContext();
  if (claims?.user_id) redirect('/dashboard');

  return tenant.config.enableRegistration ? (
    <RegistrationForm />
  ) : (
    <ErrorPage
      error="Registrace je uzavřena"
      details="Nové registrace aktuálně nepřijímáme."
    />
  );
}
