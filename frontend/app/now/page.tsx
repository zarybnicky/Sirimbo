import type { Metadata } from 'next';

import NowPage from './NowPageClient';

export const metadata: Metadata = {
  title: 'Právě probíhá',
  robots: { index: false, follow: false },
};

export default function Page() {
  return <NowPage />;
}
