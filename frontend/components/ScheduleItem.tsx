import * as React from 'react';
import { ScheduleFragment } from 'lib/graphql/Schedule';
import { LessonButton } from 'components/LessonButton';
import { Card } from 'components/Card';
import { Schedule } from 'lib/entities';
import { useAuth } from 'lib/data/use-auth';

export const ScheduleItem = ({ item }: { item: ScheduleFragment }) => {
  const { perms } = useAuth();

  return (
    <Card
      menu={Schedule.useMenu(item)}
      className="group min-w-[200px] w-72 rounded-lg border-stone-200 border"
    >
      <div className="ml-3 mb-0.5">
        <div className="text-sm text-stone-500">{item.rKde}</div>
        <div className="text-xl">{item.userByRTrener?.fullName}</div>
      </div>
      <div className="flex gap-[1px] flex-col">
        {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => (
          <LessonButton key={i} schedule={item} lesson={lesson} />
        ))}
      </div>
    </Card>
  );
};
