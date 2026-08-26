/* eslint-disable import-x/no-unused-modules */

import { LoginForm } from './LoginForm';
import { cardCls } from '@/ui/style';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Přihlášení',
};

export default function LoginPage() {
  return (
    <div className={cardCls()}>
      <LoginForm />
    </div>
  );
}
