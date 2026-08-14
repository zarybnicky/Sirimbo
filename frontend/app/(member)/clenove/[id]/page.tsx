/* eslint-disable import-x/no-unused-modules */
import { PersonView } from '@/ui/PersonView';
import { CornerLeftUp } from 'lucide-react';
import type { Metadata } from 'next';
import Link from 'next/link';

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

  return (
    <>
      <div className="lg:hidden pt-4">
        <Link href="/clenove" className="flex gap-2 pt-4">
          <CornerLeftUp className="size-4" />
          Zpět na seznam
        </Link>
      </div>

      <PersonView id={id} />
    </>
  );
}
