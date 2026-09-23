
import { cardCls } from '@/ui/style';
import { Metadata } from 'next';
import { ForgottenPasswordForm } from './ForgottenPasswordForm';
import { getRequestAuth } from '@/lib/server/tenant';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Zapomenuté heslo',
};

export default async function ForgottenPasswordPage() {
  const { user } = await getRequestAuth();
  if (user) redirect(user.userProxiesList.length > 0 ? '/dashboard' : '/profil');

  return (
    <div className={cardCls({ className: 'p-4 max-w-lg' })}>
      <ForgottenPasswordForm />
    </div>
  );
}
