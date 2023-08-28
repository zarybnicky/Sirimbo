import { Card } from '@app/ui/Card';
import { RichTextView } from '@app/ui/RichTextView';
import * as React from 'react';
import { CohortFragment } from '@app/graphql/Cohorts';
import Link from 'next/link';

export function CohortItem({ item }: { item: CohortFragment }) {
  return (
    <Card cohort={item} className="group break-inside-avoid">
      <h5 className="text-xl underline">
        <Link href={`/treninkove-skupiny/${item.id}`}>{item.sName}</Link>
      </h5>
      <h6 className="font-bold mb-2">{item.sLocation}</h6>
      <RichTextView
        value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
      />
    </Card>
  );
}
