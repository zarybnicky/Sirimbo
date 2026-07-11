/* eslint-disable import-x/no-unused-modules */
import { Providers } from '@/ui/Providers';
import type { Metadata } from 'next';
import type { ReactNode } from 'react';

export const metadata: Metadata = {
  title: 'Přihlášení',
  robots: {
    index: false,
    follow: false,
  },
};

export default function AuthLayout({ children }: { children: ReactNode }) {
  return <Providers>{children}</Providers>;
}
