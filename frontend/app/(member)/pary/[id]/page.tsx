/* eslint-disable import-x/no-unused-modules */
import type { Metadata } from 'next';
import { Couple } from './Couple';

export const metadata: Metadata = {
  title: 'Pár',
  robots: { index: false, follow: false },
};

export default async function CouplePage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;
  return <Couple id={id} />;
}
