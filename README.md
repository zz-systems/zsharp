# zsharp
experimental .NET language playground

Should look like this at some point
    using System.IO;

    namespace Test:
    	public module Program:
    		
    		[EntryPoint]
    		public fn Main(string[] args):
    			Console.WriteLine("start");
    			
    			foreach item in args:
    				Console.WriteLine(item);
    			
    			sequential foreach item in args:
    				Console.WriteLine(item);
    				
    			//  mapped to Parallel.Foreach();
    			parallel foreach (item, index) in args:
    				Console.WriteLine(item + " at " + index);
    				
    			for (i = 0; i < 10; i++):
    				Console.WriteLine(i² + i³);
    				
    			parallel for (i = 0; i < 10; i++):
    				Console.WriteLine(i² + i³);
    				
    			var list = [1,2,3,4,5, 2, 2 ,2 ,2 ,2 ,3 ,4 ,5 ,5 , 6];
    			var dict = [{Guid.Empty, "löl"}]
    			
    			Console.WriteLine(dict[Guid.Empty]);			
    			
    			parallel foreach group in (group list by i => i):
    				parallel foreach item in group:
    					Console.WriteLine(item + ", " group.Count())
    					
    			if true:
    				Console.WriteLine("Darth Kebab");
    			else:
    				return 0;
    				
    				
    			while true: Console.WriteLine("HAHA!");
    			
    			return 1;			
    		
    		public proxy int fn<T> index_of,in(T needle, IEnumerable<T> haystack): haystack.IndexOf(needle);
    			
    		public proxy T fn<T> find_in,by(IEnumerable<T> source, fn<T, bool> selector): source.Find(selector);
    			
    		public proxy IGrouping<TKey, T> fn<TKey, T> group,by(IEnumerable<T> source, fn<TKey> selector): source.GroupBy(keySelector);
    			
    		private int _value;
    		
    		public int Value: _value;
    			get: return _value;
    			set: _value = value;
    	
    	
    	public type ViewModel<TModel>
    		using VMBase<TModel>, INotifyPropertyChanged 
    		where TModel is new:
    		
    		private TModel _context;
    		
    		public new (TModel context):
    			_context = context;
    		
    		[Notify]
    		public proxy int Value: _context.Value;

	
