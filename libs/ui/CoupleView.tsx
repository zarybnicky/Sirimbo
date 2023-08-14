import React from 'react';
import { CoupleDocument } from '@app/graphql/Couple';
import { TitleBar } from '@app/ui/TitleBar';
import { useQuery } from 'urql';

export function CoupleView({id}: {id: string}) {
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;
  if (!item) {
    return null;
  }
  return (
    <div className="container py-4">
      <TitleBar title={`${item.man?.firstName} ${item.man?.lastName} - ${item.woman?.firstName} ${item.woman?.lastName}`}>
        {/* <DeleteButton
          doc={DeleteCoupleDocument}
          id={id}
          redirect={'/pary'}
          title="smazat pár"
        /> */}
      </TitleBar>
      {/* Poslední lekce
      {item.eventRegistrations?.nodes.map((item) => (
        <LessonButton
          key={item.id}
          schedule={item.event}
          lesson={item}
          showTrainer
          showDate
        />
      ))} */}
    </div>
  );
}
