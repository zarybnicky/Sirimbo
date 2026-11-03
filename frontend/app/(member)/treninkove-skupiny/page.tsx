import type { Metadata } from 'next';
import { Cohorts } from './Cohorts';

export const metadata: Metadata = {
  title: 'Tréninkové skupiny',
  description:
    'Přehled tréninkových skupin včetně míst tréninků a zaměření jednotlivých skupin.',
  alternates: { canonical: '/treninkove-skupiny' },
};

export default function CohortsPage() {
  return <Cohorts />;
}
