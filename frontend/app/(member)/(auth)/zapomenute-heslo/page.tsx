/* eslint-disable import-x/no-unused-modules */

import { cardCls } from '@/ui/style';
import { ForgottenPasswordForm } from '@/ui/forms/ForgottenPasswordForm';
import { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Zapomenuté heslo',
};

export default function ForgottenPasswordPage() {
  return (
    <div className={cardCls({ className: 'p-4 max-w-lg' })}>
      <ForgottenPasswordForm />
    </div>
  );
}
