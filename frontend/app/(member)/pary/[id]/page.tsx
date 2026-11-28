import { CoupleDocument } from '@/graphql/Memberships';
import { executeGraphql } from '@/lib/server/graphql';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache } from 'react';
import { Couple } from './Couple';

type Props = {
  params: Promise<{ id: string }>;
};

const getCouple = cache(async (id: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  const data = await executeGraphql(CoupleDocument, { id });
  return data.couple;
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const couple = await getCouple(id);
  if (!couple) notFound();

  return {
    title: couple.name,
    robots: { index: false, follow: false },
  };
}

export default async function CouplePage({ params }: Props) {
  const { id } = await params;
  const couple = await getCouple(id);
  if (!couple) notFound();

  return <Couple initialCouple={couple} />;
}
