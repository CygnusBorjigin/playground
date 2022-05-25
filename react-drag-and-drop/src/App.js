import DraggableList from './components/DraggableList';

function App() {
  return (
    <div>
      <div className="bg-gray-400 fixed top-0 w-screen h-16">
        <h1 className='text-center mt-4 text-2xl font-biz my-auto'> Try to guess the correct order </h1>
      </div>
      <DraggableList></DraggableList>
    </div>
  );
}

export default App;
