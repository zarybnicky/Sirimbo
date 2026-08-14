/* eslint-disable import-x/no-unused-modules */
import type { Metadata } from 'next';
import { Announcement } from './Announcement';

export const metadata: Metadata = {
  title: 'Nástěnka',
  robots: { index: false, follow: false },
};

export default async function AnnouncementPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;
  return <Announcement id={id} />;
}
