/* eslint-disable import-x/no-unused-modules */
import { CohortWithMembersDocument } from '@/graphql/Cohorts';
import { executeGraphql } from '@/lib/server/graphql';
import { slugify } from '@/lib/slugify';
import { stripHtml } from '@/lib/stripHtml';
import type { Metadata } from 'next';
import { notFound, redirect } from 'next/navigation';
import { cache } from 'react';
import { TrainingGroup } from './TrainingGroup';

type Props = {
  params: Promise<{ id: string; slug?: string[] }>;
};

const getTrainingGroup = cache(async (id: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  return executeGraphql(CohortWithMembersDocument, { id }).then((x) => x.entity);
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const cohort = await getTrainingGroup(id);
  if (!cohort) notFound();

  return {
    title: cohort.name,
    description: stripHtml(cohort.description) || cohort.name,
    alternates: {
      canonical: `/treninkove-skupiny/${cohort.id}/${slugify(cohort.name)}`,
    },
    robots:
      cohort.isVisible && !cohort.isArchived
        ? undefined
        : { index: false, follow: false },
  };
}

export default async function TrainingGroupPage({ params }: Props) {
  const { id, slug } = await params;
  const cohort = await getTrainingGroup(id);
  if (!cohort) notFound();

  const expectedSlug = slugify(cohort.name);
  if (slug?.join('/') !== expectedSlug) {
    redirect(`/treninkove-skupiny/${cohort.id}/${expectedSlug}`);
  }

  return <TrainingGroup initialCohort={cohort} />;
}
