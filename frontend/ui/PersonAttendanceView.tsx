import { PersonAttendanceDocument } from '@/graphql/Person';
import { EventButton } from '@/ui/EventButton';
import { attendanceIcons } from '@/ui/InstanceAttendanceView';
import React from 'react';
import { useQuery } from 'urql';

interface Props {
  id: string;
}

export function PersonAttendanceView({ id }: Props) {
  const [query] = useQuery({
    query: PersonAttendanceDocument,
    variables: { id },
    pause: !id,
  });
  const item = query.data?.person;
  if (!item?.eventAttendancesList) {
    return null;
  }
  const attendanceList = item.eventAttendancesList
    .filter((x) => x.status !== 'CANCELLED')
    .filter((x) => x.instance)
    .toSorted((x, y) =>
      `${x.person?.lastName}${x.person?.firstName}`.localeCompare(
        `${y.person?.lastName}${y.person?.firstName}`,
      ),
    );

  return (
    <div>
      <div className="grid grid-cols-[1fr_50px]">
        {attendanceList.map((item) => (
          <React.Fragment key={item.id}>
            <EventButton
              event={item.instance?.event!}
              instance={item.instance!}
              viewer="trainer"
              showDate
            />
            <div className="flex items-center justify-center">
              {React.createElement(attendanceIcons[item.status], {
                className: 'size-5',
              })}
            </div>
          </React.Fragment>
        ))}
      </div>
    </div>
  );
}
