/* eslint-disable import-x/no-unused-modules */

import { getRequestTenant } from '@/tenant/server';
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';
import { RegistrationForm } from './RegistrationForm';

export const metadata: Metadata = {
  title: 'Přihláška nového člena',
};

export default async function RegisterPage() {
  const tenant = await getRequestTenant();

  return tenant.config.enableRegistration ? (
    <RegistrationForm />
  ) : (
    <ErrorPage
      error="Registrace je uzavřena"
      details="Nové registrace aktuálně nepřijímáme."
    />
  );
}
