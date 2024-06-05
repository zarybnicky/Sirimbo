import { PersonAttendanceDocument } from '@/graphql/Person';
// import { AreaChart, Area, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts';
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

  if (!item) {
    return null;
  }

  return (
    <div>
      {/* <ResponsiveContainer width="100%" minHeight={200}>
        <AreaChart
          height={200}
          data={item.weeklyAttendanceList || []}
          margin={{ top: 10, right: 30, left: 0, bottom: 0 }}
        >
          <CartesianGrid strokeDasharray="3 3" />
          <XAxis dataKey="week" />
          <YAxis dataKey="eventCount" name="Účasti" />
          <Tooltip />
          <Area type="monotone" dataKey="eventCount" name="Účasti" stroke="#8884d8" fill="#8884d8" />
        </AreaChart>
      </ResponsiveContainer> */}

      <div className="grid grid-cols-[1fr_50px]">
        {item.eventAttendancesList?.map((item) =>
          !item.instance ? (
            <React.Fragment key={item.id} />
          ) : (
            <React.Fragment key={item.id}>
              <EventButton instance={item.instance} viewer="trainer" showDate />
              <div className="flex items-center justify-center">
                {React.createElement(attendanceIcons[item.status], {
                  className: 'size-5',
                })}
              </div>
            </React.Fragment>
          ),
        )}
      </div>
    </div>
  );
}
