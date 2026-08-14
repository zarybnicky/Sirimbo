/* eslint-disable import-x/no-unused-modules */
import type { Metadata } from 'next';
import { TrainingGroups } from './TrainingGroups';

export const metadata: Metadata = {
  title: 'Tréninkové skupiny',
  description:
    'Přehled tréninkových skupin včetně míst tréninků a zaměření jednotlivých skupin.',
  alternates: { canonical: '/treninkove-skupiny' },
};

export default function TrainingGroupsPage() {
  return <TrainingGroups />;
}
