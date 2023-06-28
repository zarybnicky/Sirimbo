import * as React from 'react';
import { ScheduleFragment } from '@app/graphql/Schedule';
import { LessonButton } from '@app/ui/LessonButton';
import { Card, CardMenu } from '@app/ui/Card';
import { useAuth } from './use-auth';
import { DropdownMenuLink } from './dropdown';

export const ScheduleItem = ({ item }: { item: ScheduleFragment }) => {
  const { perms } = useAuth();

  return (
    <Card className="group min-w-[200px] w-72 rounded-lg border-accent-7 border">
      {perms.canEditSchedule(item) && (
        <CardMenu>
          <DropdownMenuLink href={{ pathname: '/admin/rozpis/[id]', query: { id: item.id } }}>
            Upravit
          </DropdownMenuLink>
        </CardMenu>
      )}

      <div className="ml-3 mb-0.5">
        <div className="text-sm text-accent-11">{item.rKde}</div>
        <div className="text-xl">{item.userByRTrener?.fullName}</div>
      </div>
      <div className="space-y-[1px]">
        {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => (
          <LessonButton key={i} schedule={item} lesson={lesson} />
        ))}
      </div>
    </Card>
  );
};
