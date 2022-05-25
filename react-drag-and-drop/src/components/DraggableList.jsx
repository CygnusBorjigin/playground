import { v4 as uuidv4 } from 'uuid';
import { useEffect, useState } from 'react';

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
            }
        )
    })
    .sort((a, b) => a.sortIndex - b.sortIndex)
    .map((each_item, index) => {
        return(
            {
                value: each_item.value,
                id: uuidv4(),
                index
            }
        )
    });

    const [itemOrder, setItemOrder] = useState(displayOrder);
    const [itemBeingDragged, setItemBeingDragged] = useState("");
    const [itemBeingDropped, setItemBeingDropped] = useState("");

    const handelSwapItem = (start, end) => {
        const newList = itemOrder.map(eachItem => {
            if (eachItem.index === start){
                return {
                    value: eachItem.value,
                    id: eachItem.id,
                    index: end
                }
            } else if (eachItem.index === end){
                return {
                    value: eachItem.value,
                    id: eachItem.id,
                    index: start
                }
            } else {
                return {
                    value: eachItem.value,
                    id: eachItem.id,
                    index: eachItem.index
                }
            }
        })
        setItemOrder(newList.sort((a, b) => a.index - b.index));
        setItemBeingDragged("");
        setItemBeingDropped("");
    }

    useEffect(() => {
        const startIndex = itemOrder.find(eachItem => eachItem.id === itemBeingDragged);
        const endIndex = itemOrder.find(eachItem => eachItem.id === itemBeingDropped);
        if (startIndex && endIndex) handelSwapItem(startIndex.index, endIndex.index);
    }, [itemBeingDropped]);

    const handelDragStart = (event) => {
        setItemBeingDragged(event.target.getAttribute("value"));
    }

    const handelDrop = (event) => {
        setItemBeingDropped(event.target.getAttribute("value"));
    };

    const handelDragOver = (event) => {
        event.preventDefault();
    }

    return(
        <div className="mt-20 flex flex-row justify-center font-raleway font-bold text-xl w-screen">
            <ul className="w-1/3">
            {itemOrder.map(each_item => {
                return(
                    <li
                        className="border-2 mt-4 text-center h-16 flex flex-col justify-center"
                        draggable="true"
                        value={each_item.id}
                        key = {uuidv4()}
                        onDrag={handelDragStart}
                        onDrop={handelDrop}
                        onDragOver={handelDragOver}
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