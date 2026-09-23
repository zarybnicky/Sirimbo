
import { getRequestContext } from '@/lib/server/tenant';
import { LoginForm } from './LoginForm';
import { cardCls } from '@/ui/style';
import type { Metadata } from 'next';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Přihlášení',
};

export default async function LoginPage() {
  const { claims } = await getRequestContext();
  if (claims?.user_id) redirect('/dashboard');

  return (
    <div className={cardCls()}>
      <LoginForm />
    </div>
  );
}
