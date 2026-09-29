import type { Metadata } from 'next';
import { Announcement } from '../Announcement';

export const metadata: Metadata = {
  title: 'Upravit příspěvek',
  robots: { index: false, follow: false },
};

export default async function EditAnnouncementPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;
  return <Announcement id={id} edit />;
}
