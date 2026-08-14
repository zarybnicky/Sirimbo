/* eslint-disable import-x/no-unused-modules */
import { CohortGroupDocument } from '@/graphql/CohortGroup';
import { executeGraphql } from '@/lib/server/graphql';
import { slugify } from '@/lib/slugify';
import { stripHtml } from '@/lib/stripHtml';
import type { Metadata } from 'next';
import { notFound, redirect } from 'next/navigation';
import { cache } from 'react';
import { TrainingProgramDetails } from './TrainingProgramDetails';

type Props = {
  params: Promise<{
    id: string;
    slug?: string[];
  }>;
};

const getTrainingProgram = cache(async (id: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  return executeGraphql(CohortGroupDocument, { id }).then((x) => x.cohortGroup);
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const item = await getTrainingProgram(id);
  if (!item) notFound();

  return {
    title: item.name,
    description: stripHtml(item.description) || item.name,
    alternates: {
      canonical: `/treninkove-programy/${item.id}/${slugify(item.name)}`,
    },
    robots: item.isPublic ? undefined : { index: false, follow: false },
  };
}

export default async function TrainingProgramPage({ params }: Props) {
  const { id, slug } = await params;
  const item = await getTrainingProgram(id);
  if (!item) notFound();

  const expectedSlug = slugify(item.name);
  if (slug?.join('/') !== expectedSlug) {
    redirect(`/treninkove-programy/${item.id}/${expectedSlug}`);
  }

  return <TrainingProgramDetails initialItem={item} />;
}
