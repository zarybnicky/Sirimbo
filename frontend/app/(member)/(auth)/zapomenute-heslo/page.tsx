
import { cardCls } from '@/ui/style';
import { Metadata } from 'next';
import { ForgottenPasswordForm } from './ForgottenPasswordForm';
import { getRequestContext } from '@/lib/server/tenant';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Zapomenuté heslo',
};

export default async function ForgottenPasswordPage() {
  const { claims } = await getRequestContext();
  if (claims?.user_id) redirect('/dashboard');

  return (
    <div className={cardCls({ className: 'p-4 max-w-lg' })}>
      <ForgottenPasswordForm />
    </div>
  );
}
