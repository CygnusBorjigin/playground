import { v4 as uuidv4 } from 'uuid';
import { useState } from 'react';

const DraggebleList = () => {
    const correctOrderedList = [
        "Someone",
        "Somewhere",
        "doing",
        "Something",
        "and",
        "Something else",
        "along",
        "with",
        "Some other things"
    ];

    const displayOrder = correctOrderedList.map((each_item, index) => {
        return(
            {
                value: each_item,
                sortIndex: Math.random(),
                id: uuidv4(),
            }
        )
    })
    .sort((a, b) => a.sortIndex - b.sortIndex)
    .map((each_item, index) => {
        return(
            {
                value: each_item.value,
                id: each_item.id,
                index
            }
        )
    });

    const [itemOrder, setItemOrder] = useState(displayOrder);

    return(
        <div className="mt-20 flex flex-row justify-center font-raleway font-bold text-xl w-screen">
            <ul className="w-1/3">
            {itemOrder.map(each_item => {
                return(
                    <li
                        className="border-2 mt-4 text-center h-16 flex flex-col justify-center"
                        draggable="true"
                        key = {uuidv4()}
                    >
                        {each_item.value}
                    </li>
                )
            })}
            </ul>
        </div>
    )
};

export default DraggebleList;