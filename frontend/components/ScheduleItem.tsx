import * as React from 'react';
import { usePermissions } from 'lib/data/use-permissions';
import { ScheduleFragment } from 'lib/graphql/Schedule';
import { Dropdown } from 'components/Dropdown';
import { LessonButton } from 'components/LessonButton';
import { Card } from 'components/Card';

export const ScheduleItem = ({ item }: { item: ScheduleFragment; }) => {
  const perms = usePermissions();

  return (
    <div className="group relative min-w-[200px]">
      <div className="ml-3 mb-0.5">
        {perms.canEditSchedule(item) && (
          <Dropdown
            className="absolute right-1 top-1"
            align="end"
            options={[{ title: "Upravit", href: `/admin/rozpis/${item.id}` }]}
          />
        )}

        <div className="text-sm text-stone-500">{item.rKde}</div>
        <div className="text-xl">{item.userByRTrener?.fullName}</div>
      </div>

      <Card className="grid mx-auto w-72 rounded-lg border-stone-200 border">
        {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => (
          <LessonButton key={i} schedule={item} lesson={lesson} />
        ))}
      </Card>
    </div>
  );
};
