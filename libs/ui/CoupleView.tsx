import React from 'react';
import { CoupleDocument, DeleteCoupleDocument } from '@app/graphql/Couple';
import { DeleteButton } from '@app/ui/DeleteButton';
import { LessonButton } from '@app/ui/LessonButton';
import { TitleBar } from '@app/ui/TitleBar';
import { formatCoupleName } from '@app/ui/format-name';
import { useQuery } from 'urql';

export function CoupleView({id}: {id: string}) {
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.pary;
  if (!item) {
    return null;
  }
  return (
    <div className="container py-4">
      <TitleBar backHref="/admin/pary" title={formatCoupleName(item)}>
        <DeleteButton
          doc={DeleteCoupleDocument}
          id={id}
          redirect={'/admin/nastenka'}
          title="smazat pár"
        />
      </TitleBar>
      Poslední lekce
      {item.rozpisItemsByRiPartner?.nodes.map((item) => (
        <LessonButton
          key={item.id}
          schedule={item.rozpiByRiIdRodic!}
          lesson={item}
          showTrainer
          showDate
        />
      ))}
    </div>
  );
}
