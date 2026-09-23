
import { getRequestAuth, getRequestContext } from '@/lib/server/tenant';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import { RegistrationForm } from './RegistrationForm';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Přihláška nového člena',
};

export default async function RegisterPage() {
  const [{ tenant }, { user }] = await Promise.all([
    getRequestContext(),
    getRequestAuth(),
  ]);
  if (user) redirect(user.userProxiesList.length > 0 ? '/dashboard' : '/profil');

  return tenant.config.enableRegistration ? (
    <RegistrationForm />
  ) : (
    <ErrorPage
      error="Registrace je uzavřena"
      details="Nové registrace aktuálně nepřijímáme."
    />
  );
}
