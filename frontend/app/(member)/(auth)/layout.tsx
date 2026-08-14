/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import type { ReactNode } from 'react';

export const metadata: Metadata = {
  robots: { index: false, follow: false },
};

export default function AuthLayout({ children }: { children: ReactNode }) {
  return (
    <Layout className="grow content relative content-stretch">
      <div className="flex h-[calc(100dvh-80px)] w-full items-center justify-center bg-neutral-1 p-5">
        {children}
      </div>
    </Layout>
  );
}
