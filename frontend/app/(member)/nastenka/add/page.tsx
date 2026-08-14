/* eslint-disable import-x/no-unused-modules */
import type { Metadata } from 'next';
import { CreateAnnouncement } from './CreateAnnouncement';

export const metadata: Metadata = {
  title: 'Nový příspěvek',
  robots: { index: false, follow: false },
};

export default function CreateAnnouncementPage() {
  return <CreateAnnouncement />;
}
