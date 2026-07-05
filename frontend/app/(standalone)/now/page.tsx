/* eslint-disable import-x/no-unused-modules */
import type { Metadata } from 'next';

import NowPage from './NowPageClient';

export const metadata: Metadata = {
  title: 'Právě probíhá',
};

export default function Page() {
  return <NowPage />;
}
