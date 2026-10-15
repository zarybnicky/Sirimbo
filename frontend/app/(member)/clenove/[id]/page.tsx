import { PersonView } from '@/ui/PersonView';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Člen',
  robots: { index: false, follow: false },
};

export default async function MemberPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;

  return <PersonView id={id} parent={{ label: 'Členové', href: '/clenove' }} />;
}
