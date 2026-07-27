/* eslint-disable import-x/no-unused-modules */
import { Providers } from '@/ui/Providers';
import type { ReactNode } from 'react';

export default function MemberLayout({ children }: { children: ReactNode }) {
  return <Providers>{children}</Providers>;
}
