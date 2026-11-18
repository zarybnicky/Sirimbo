import { getRequestContext } from '@/lib/server/tenant';
import { LoginForm } from './LoginForm';
import { cardCls } from '@/ui/style';
import type { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { sanitizeReturnURL } from '@/lib/sanitize';

export const metadata: Metadata = {
  title: 'Přihlášení',
};

export default async function LoginPage({
  searchParams,
}: {
  searchParams: Promise<{ from?: string }>;
}) {
  const { auth } = await getRequestContext();
  const { from } = await searchParams;
  if (auth.isLoggedIn)
    redirect(sanitizeReturnURL(from, 'https://app.invalid') ?? '/dashboard');

  return (
    <div className={cardCls()}>
      <LoginForm />
    </div>
  );
}
