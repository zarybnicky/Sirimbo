import { PersonMembershipsDocument } from '@/graphql/Person';
import { executeGraphql } from '@/lib/server/graphql';
import { PersonView } from '@/ui/PersonView';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache } from 'react';

type Props = {
  params: Promise<{ id: string }>;
};

const getPerson = cache(async (id: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  const data = await executeGraphql(PersonMembershipsDocument, { id });
  return data.person;
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const person = await getPerson(id);
  if (!person) notFound();

  return {
    title: person.name,
    robots: { index: false, follow: false },
  };
}

export default async function MemberPage({ params }: Props) {
  const { id } = await params;
  const person = await getPerson(id);
  if (!person) notFound();

  return (
    <PersonView
      initialPerson={person}
      parent={{ title: 'Členové', href: '/clenove' }}
    />
  );
}
