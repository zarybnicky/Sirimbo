import { getRequestContext } from '@/lib/server/tenant';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import { RegistrationForm } from './RegistrationForm';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Přihláška nového člena',
};

export default async function RegisterPage() {
  const { tenant, auth } = await getRequestContext();
  if (auth.isLoggedIn) redirect('/dashboard');

  return tenant.config.enableRegistration ? (
    <RegistrationForm />
  ) : (
    <ErrorPage
      error="Registrace je uzavřena"
      details="Nové registrace aktuálně nepřijímáme."
    />
  );
}
