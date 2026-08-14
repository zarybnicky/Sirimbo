/* eslint-disable import-x/no-unused-modules */

import { LoginForm } from './LoginForm';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Přihlášení',
};

export default function LoginPage() {
  return (
    <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
      <LoginForm />
    </div>
  );
}
